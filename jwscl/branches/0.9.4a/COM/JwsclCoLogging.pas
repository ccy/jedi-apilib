unit JwsclCoLogging;
{$WARN SYMBOL_PLATFORM OFF}
interface

uses
  Dialogs, Classes, ComObj, SysUtils, ActiveX, StdVcl, JwsclLogging, JWSCLCom_TLB,
  JwsclStrings;


type
  TJwLogClientImpl = class(TAutoObject, IJwLogClient)
  protected
    fInternalObject : JwsclLogging.IJwLogClient;
  protected
    procedure Log(LogType: JwEnumLogType; const LogMessage: WideString); safecall;
    procedure Signal(LogType: JwEnumSignalType;
      const Source: WideString; const Target: WideString;
      const LogMessage: WideString); safecall;
    procedure Memory(LogType: JwEnumMemoryType; const MemType: WideString; const LogMessage: WideString); safecall;
    procedure Exception(Exception: OleVariant); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

  {@Name implements IJwLogServer for internal use}
  TJwLogServerImpl = class(TAutoObject, IJwLogServer)
  protected
    fInternalObject : JwsclLogging.IJwLogServer;
    fWriterClass : JwsclLogging.IJwWriterClass;
    fCOMWriterClass : JWSCLCom_TLB.IJwWriterClass;
  protected
    function Get_WriterClass: IJwWriterClass; safecall;
    procedure Set_WriterClass(const Value: IJwWriterClass); safecall;
    function Get_LogTypes: OleVariant; safecall;
    procedure Set_LogTypes(Value: OleVariant); safecall;
    function Connect(EnterType: JwEnumEnterType; const ClassName: WideString;
                     const MethodName: WideString; const FileName: WideString;
                     const LogMessage: WideString): IJwLogClient; safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;


   TInternalJwXMLAttribute = class(TAutoObject, IJwXMLAttribute)
   protected
     fName,
     fValue : WideString;
   protected
     function Get_Name: WideString; safecall;
     procedure Set_Name(const Value: WideString); safecall;
     function Get_Value: WideString; safecall;
     procedure Set_Value(const Value: WideString); safecall;
     function ToString: WideString; safecall;
   end;

   TInternalJwEventType = class(TAutoObject, IJwEventType)
   protected
     fTagName   : JwXMLLogTag;
     fTypeValue : SYSINT;
   protected
     function Get_TagName: JwXMLLogTag; safecall;
     procedure Set_TagName(Value: JwXMLLogTag); safecall;
     function Get_TypeValue: SYSINT; safecall;
     procedure Set_TypeValue(Value: SYSINT); safecall;

     function ToString: WideString; safecall;

   end;

   TInternalWriterClass = class(TInterfacedObject,JwsclLogging.IJwWriterClass)
   protected
     fInternalObject : JWSCLCom_TLB.IJwWriterClass;
   protected
     function WriteSingleTag(IndentLevel: Integer; const TagName: TJwString;
                             const Value: TJwString; Attributes: TJwXMLAttributes): TJwString; safecall;

     function StartWriteMultipleTags(IndentLevel: Integer; const TagName: TJwString;
                                     Attributes: TJwXMLAttributes): TJwString; safecall;
     function EndWriteMultipleTags: TJwString; safecall;

     procedure Done; safecall;

     function CreateObject : JwsclLogging.IJwWriterClass; safecall;
   end;

   {Delphi only
    @Name converts an array of TJwEventType (TJwEventTypes) from unit JwsclLogging
    into a COM array of IJwEventType.
   }
   function JwEventTypesToVariant(const Events : TJwEventTypes) : Variant; stdcall;
   function JwVariantToEventTypes(const Events : Variant) : TJwEventTypes; stdcall;


   function JwXMLAttributesToVariant(const Attributes: TJwXMLAttributes) : Variant;
   function JwVariantToXMLAttributes(const Attributes: Variant) : TJwXMLAttributes;



implementation
uses ComServ,
     JwsclUtils, JwsclExceptions, Variants,
     JwsclCOMExports, JwsclTypes,
     JWSCLCoException;


function JwXMLAttributesToVariant(const Attributes: TJwXMLAttributes) : Variant;
var Attr : IJwXMLAttribute;
    i : Integer;
begin
  Result := VarArrayCreate([low(Attributes),high(Attributes)],varUnknown);

  for i := low(Attributes) to high(Attributes) do
  begin
    Attr := TInternalJwXMLAttribute.Create;
    Attr.Name := Attributes[i].Name;
    Attr.Value := Attributes[i].Value;

    Result[i] := Attr as IJwXmlAttribute;
  end;
end;

function JwVariantToXMLAttributes(const Attributes: Variant) : TJwXMLAttributes;
var i : Integer;
begin
  result := nil;
  if VarType(Attributes) = varNull then
    exit;
  ASSERT(VarArrayLowBound(Attributes, 1) = 0);

  SetLength(result, VarArrayHighBound(Attributes, 1)+1);

  for i := 0 to high(result) do
  begin
    result[i].Name  := (IUnknown(Attributes[i]) as IJwXMLAttribute).Name;
    result[i].Value := (IUnknown(Attributes[i]) as IJwXMLAttribute).Value;
  end;


end;



function JwEventTypesToVariant(const Events : TJwEventTypes) : Variant; stdcall;
var i : Integer;
    EventType : IJwEventType;
    V : IJwEventType;
begin
  result := Null;
  if Length(Events) = 0 then
    exit;

  result := VarArrayCreate([0,High(Events)],varUnknown);
  for i := 0 to High(Events) do
  begin
    V := CreateComObject(CLASS_JwEventType) as IJwEventType;
    V.TypeValue := Events[i].TypeValues;
    V.TagName := TOleEnum(Events[i].TagName);
    result[i] := V as IJwEventType;
   // ShowMessage((IUnknown(result[i]) as IJwEventType).ToString);
  end;
end;

function JwVariantToEventTypes(const Events : Variant) : TJwEventTypes; stdcall;
var i : Integer;
    EventType : IJwEventType;
    S : IUnknown;
begin
  if VarType(Events) = varNull then
  begin
    result := nil;
    exit;
  end;

  SetLength(result, VarArrayHighBound(Events,1)+1);
  for i := 0 to High(result) do
  begin

    EventType := (IUnknown(Events[i]) as IJwEventType);

    result[i].TagName := TJwXMLLogTag(EventType.TagName);
    result[i].TypeValues := EventType.TypeValue;
  end;
end;



{ TJwLogServerImpl }

function TJwLogServerImpl.Connect(EnterType: JwEnumEnterType;
  const ClassName, MethodName, FileName,
  LogMessage: WideString): IJwLogClient;
var I : TJwLogClientImpl;
begin
  I := TJwLogClientImpl.Create;
  I.fInternalObject := fInternalObject.Connect(OleVariant(EnterType),
    ClassName, MethodName, FileName, LogMessage);

  result := I as IJwLogClient;
end;

destructor TJwLogServerImpl.Destroy;
begin

  inherited;
end;

function TJwLogServerImpl.Get_LogTypes: OleVariant;
var i : Integer;
    Events : TJwEventTypes;
    EventType : IJwEventType;
    V : TInternalJwEventType;
begin
  result := Null;
  Events := fInternalObject.GetLogTypes;
  if Length(Events) = 0 then
    exit;

  {result := VarArrayCreate([0,High(Events)],varUnknown);
  for i := 0 to High(Events) do
  begin
    V := TInternalJwEventType.Create;
    V.fTypeValue := Events[i].TypeValues;
    V.fTagName := TOleEnum(Events[i].TagName);
    result[i] := V as IJwEventType;
  end;}
  result := JwEventTypesToVariant(Events);
end;

function TJwLogServerImpl.Get_WriterClass: IJwWriterClass;
begin
  result := fCOMWriterClass;
end;

procedure TJwLogServerImpl.Initialize;
var L : JwsclLogging.TJwEventTypes;
begin
  inherited;

  SetLength(L,0);
  fInternalObject := CreateLogServer(nil, L, nil);
end;

procedure TJwLogServerImpl.Set_LogTypes(Value: OleVariant);
var i : Integer;
    Events : TJwEventTypes;
    EventType : IJwEventType;
begin
  if VarType(Value) = varNull then
  begin
    Events := nil;
    fInternalObject.SetLogTypes(Events);
    exit;
  end;
 {
  SetLength(Events, VarArrayHighBound(Value,1)+1);
  for i := 0 to High(Events) do
  begin
    EventType := (IUnknown(Value[i]) as IJwEventType);

    Events[i].TagName := TJwXMLLogTag(EventType.TagName);
    Events[i].TypeValues := EventType.TypeValue;
  end;

  fInternalObject.SetLogTypes(Events);
  Events := nil;     }
  fInternalObject.SetLogTypes(JwVariantToEventTypes(Value));
end;

procedure TJwLogServerImpl.Set_WriterClass(const Value: IJwWriterClass);
Var I : TInternalWriterClass;
begin
  I := nil;
  if Assigned(Value) then
  begin
    I := TInternalWriterClass.Create;
    I.fInternalObject := Value;
    fWriterClass := I as JwsclLogging.IJwWriterClass;
  end;

  fCOMWriterClass := Value;

  fInternalObject.SetWriterClass(I);
end;

{ TJwLogClientImpl }

destructor TJwLogClientImpl.Destroy;
begin
  inherited;
end;

procedure TJwLogClientImpl.Exception(Exception: OleVariant);
begin
  //fInternalObject.Exception();
  raise EOleSysError.Create('Exceptions are not supported through COM at the moment.',E_NOTIMPL,0);
end;

procedure TJwLogClientImpl.Initialize;
begin
  inherited;

end;

procedure TJwLogClientImpl.Log(LogType: JwEnumLogType;
  const LogMessage: WideString);
begin
  fInternalObject.Log(TJwLogType(LogType), LogMessage);
end;

procedure TJwLogClientImpl.Memory(LogType: JwEnumMemoryType; const MemType: WideString;
  const LogMessage: WideString);
begin
  fInternalObject.Memory(JwsclLogging.TJwMemoryType(LogType), TJwstring(MemType), TJwstring(LogMessage));
end;

procedure TJwLogClientImpl.Signal(LogType: JwEnumSignalType;
  const Source: WideString; const Target: WideString;
  const LogMessage: WideString);
begin
  fInternalObject.Signal(JwsclLogging.TJwSignalType(LogType),
    Source, Target, LogMessage);
end;



{ TInternalWriterClass }

function TInternalWriterClass.CreateObject: JwsclLogging.IJwWriterClass;
var V : TInternalWriterClass;
begin
  V := TInternalWriterClass.Create;
  V.fInternalObject := fInternalObject;
  result := V as JwsclLogging.IJwWriterClass;
end;

procedure TInternalWriterClass.Done;
begin
  fInternalObject.Done;
end;

function TInternalWriterClass.EndWriteMultipleTags: TJwString;
begin
  result := fInternalObject.EndWriteMultipleTags;
end;

function TInternalWriterClass.StartWriteMultipleTags(IndentLevel: Integer;
  const TagName: TJwString; Attributes: TJwXMLAttributes): TJwString;
var Attr : IJwXMLAttribute;
    i : Integer;
    Arr : Variant;
begin
 { Arr := VarArrayCreate([low(Attributes),high(Attributes)],varUnknown);
  for i := low(Attributes) to high(Attributes) do
  begin
    Attr := TInternalJwXMLAttribute.Create;
    Attr.Name := Attributes[i].Name;
    Attr.Value := Attributes[i].Value;

    Arr[i] := Attr as IJwXmlAttribute;
  end;   }
//  JwVariantToXMLAttributes
  Arr := JwXMLAttributesToVariant(Attributes);

  {
  }

  result := fInternalObject.StartWriteMultipleTags(IndentLevel,
    TagName, Arr);
end;

function TInternalWriterClass.WriteSingleTag(IndentLevel: Integer;
  const TagName, Value: TJwString;
  Attributes: TJwXMLAttributes): TJwString;
var Attr : IJwXMLAttribute;
    i : Integer;
    Arr : Variant;
begin
  {Arr := VarArrayCreate([low(Attributes),high(Attributes)],varUnknown);
  for i := low(Attributes) to high(Attributes) do
  begin
    Attr := TInternalJwXMLAttribute.Create;
    Attr.Name := Attributes[i].Name;
    Attr.Value := Attributes[i].Value;

    Arr[i] := Attr as IJwXmlAttribute;
  end;
  }

  Arr := JwXMLAttributesToVariant(Attributes);

  result := fInternalObject.WriteSingleTag(IndentLevel,
    TagName, Value, Arr);
end;

{ TInternalJwXMLAttribute }

function TInternalJwXMLAttribute.Get_Name: WideString;
begin
  result := fName;
end;

function TInternalJwXMLAttribute.Get_Value: WideString;
begin
  result := fValue;
end;

procedure TInternalJwXMLAttribute.Set_Name(const Value: WideString);
begin
  fName := Value;
end;

procedure TInternalJwXMLAttribute.Set_Value(const Value: WideString);
begin
  fValue := Value;

end;

function TInternalJwXMLAttribute.ToString: WideString;
begin
  result := WideFormat('Name: "%s", Value: "%s"', [fNAme, fValue]);
end;

{ TInternalJwEventType }

function TInternalJwEventType.Get_TagName: JwXMLLogTag;
begin
  result := fTagName;
end;

function TInternalJwEventType.Get_TypeValue: SYSINT;
begin
  result := fTypeValue;
end;

procedure TInternalJwEventType.Set_TagName(Value: JwXMLLogTag);
begin
  fTagName := Value;
end;

procedure TInternalJwEventType.Set_TypeValue(Value: SYSINT);
begin
  fTypeValue := Value;
end;


function TInternalJwEventType.ToString: WideString;
begin
  try
    result := WideFormat('TagName: "%s", TypeValue: "%d"',
      [JwXMLLogTagString[TJwXMLLogTag(fTagName)], fTypeValue]);
  except
    result := WideFormat('TagName: "%d", TypeValue: "%d"',
      [fTagName, fTypeValue]);
  end;
end;


end.
