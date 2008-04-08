unit JwsclCoLoggingTests;

interface

uses
  ComObj, ActiveX, StdVcl, Classes, JwsclLogging, JWSCLCom_TLB,
  JwaWindows,JwaVista, TypInfo, ComLib, Variants,
  JWSCLSid, JwsclMapping, JwsclCoLogging,
  TestFrameWork;

type
  TJwLogClientImplTests = class(TTestCase)
  private

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestConnect;
    procedure TestLog;
    procedure TestSignal;
    procedure TestMemory;
    procedure TestException;
    procedure TestSetLogTypes;
    procedure TestSetWriterClass;

    procedure Test;

  end;



implementation
uses SysUtils,Dialogs;

{ TJwLogClientImplTests }

type TestType = (
      ttConnect,
      ttLog,
      ttSignal,
      ttMemory,
      ttException,
      ttSetLogTypes,
      ttSetWriterClass);

var
   TestState : TestType = ttConnect;
   CurrentTest : TJwLogClientImplTests;

type
  TCustomWriterClass = class(TInterfacedObject, IJwWriterClass)
    function WriteSingleTag(IndentLevel: SYSINT; const TagName: WideString;
                            const Value: WideString; Attributes: OleVariant): WideString; safecall;
    function StartWriteMultipleTags(IndentLevel: SYSINT; const TagName: WideString;
                                    Attributes: OleVariant): WideString; safecall;
    function EndWriteMultipleTags: WideString; safecall;
    procedure Done; safecall;
    function CreateObject: IJwWriterClass; safecall;
  end;

procedure TJwLogClientImplTests.SetUp;
begin
  inherited;
  CurrentTest := Self;
end;

procedure TJwLogClientImplTests.TearDown;
begin
  inherited;

end;

procedure TJwLogClientImplTests.Test;
var Server : IJwLogServer;
  procedure Test2();
  var C : IJwLogClient;
  begin
    C := Server.Connect(etFunction,'','Test2','','123');
    C.Log(lsMessage, '123');

  end;


var EventTypes : TJwEventTypes;
    V : OleVariant;
begin
  //TJwLogWriter.AddEventType(EventTypes, JwsclLogging.ltLog, -1);
  //TJwLogWriter.AddEventType(EventTypes, ltLog, {Integer(ltLog) or Integer(lsMessage) or }Integer(lsWarning));
  //TJwLogWriter.AddEventType(EventTypes, ltException, -1);
  //TJwLogWriter.AddEventType(EventTypes, JwsclLogging.ltEnter, -1);
  TJwLogWriter.AddEventType(EventTypes, JwsclLogging.ltDisabled, -1);
  //TJwLogWriter.AddEventType(EventTypes, ltLeave, -1);


  Server := CoJwLogServer.Create;
  Server.WriterClass := TCustomWriterClass.Create as IJwWriterClass;
  V := JwEventTypesToVariant(EventTypes);
  Server.LogTypes := V;
  Test2;

  
end;


procedure TJwLogClientImplTests.TestConnect;
var Server : IJwLogServer;
    C : IJwLogClient;
begin
  TestState := ttConnect;

  Server := CoJwLogServer.Create;
  Server.WriterClass := TCustomWriterClass.Create as IJwWriterClass;
  
  C := Server.Connect(etFunction,'1','2','3','4');
end;

procedure TJwLogClientImplTests.TestException;
var Server : IJwLogServer;
    C : IJwLogClient;
begin
  TestState := ttConnect;

  Server := CoJwLogServer.Create;
  Server.WriterClass := TCustomWriterClass.Create as IJwWriterClass;
  
  C := Server.Connect(etFunction,'1','2','3','4');
  try
    C.Exception(null);
    Check(false, 'Exception should not be supported at the moment');
  except
   
  end;
end;

procedure TJwLogClientImplTests.TestLog;
var Server : IJwLogServer;
    C : IJwLogClient;
begin
  TestState := ttConnect;

  Server := CoJwLogServer.Create;
  Server.WriterClass := TCustomWriterClass.Create as IJwWriterClass;
  
  C := Server.Connect(etFunction,'1','2','3','4');
  C.Log(lsMessage,'123');
end;

procedure TJwLogClientImplTests.TestMemory;
begin

end;

procedure TJwLogClientImplTests.TestSetLogTypes;
begin

end;

procedure TJwLogClientImplTests.TestSetWriterClass;
begin

end;

procedure TJwLogClientImplTests.TestSignal;
begin

end;

{ TCustomWriterClass }

function TCustomWriterClass.CreateObject: IJwWriterClass;
begin
  result := TCustomWriterClass.Create;
end;

procedure TCustomWriterClass.Done;
begin
   //
end;

function TCustomWriterClass.EndWriteMultipleTags: WideString;
begin
  //
end;


function ConvertIJwXMLAttributeToStruct(const Attr : IJwXMLAttribute) : TJwXMLAttribute;
begin
  result.Name := Attr.Name;
  result.Value := Attr.Value;
end;

function TCustomWriterClass.StartWriteMultipleTags(IndentLevel: SYSINT;
  const TagName: WideString; Attributes: OleVariant): WideString;
var i : Integer;
begin
  for i := VarArrayLowBound(Attributes,1) to VarArrayHighBound(Attributes,1) do
  begin
   // ShowMessage((IUnknown(Attributes[i]) as IJwXmlAttribute).ToString);
  end;
 
end;

function TCustomWriterClass.WriteSingleTag(IndentLevel: SYSINT;
  const TagName, Value: WideString; Attributes: OleVariant): WideString;
var i : Integer;
    Attr : TJwXMLAttributes;
begin
  case TestState of
    ttConnect :
      begin
       // CurrentTest.Check(Tag);
        Attr := JwVariantToXMLAttributes(Attributes);
      end;
  end;

  for i := VarArrayLowBound(Attributes,1) to VarArrayHighBound(Attributes,1) do
  begin
   // ShowMessage((IUnknown(Attributes[i]) as IJwXmlAttribute).ToString);
  end;
 // SetErrorInfo()
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TJwLogClientImplTests.Suite);

end.
 