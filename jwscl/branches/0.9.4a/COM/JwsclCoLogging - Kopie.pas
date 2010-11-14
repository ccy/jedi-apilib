unit JwsclCoLogging;

interface

uses
  Dialogs, Classes, ComObj, ComServ, SysUtils, ActiveX, StdVcl, JwsclLogging, JWSCLCom_TLB;


type
  TJwLogClientImpl = class(TAutoObject, JWSCLCom_TLB.IJwLogClient)
  protected
    fInternalObject : JwsclLogging.IJwLogClient;
  protected
    procedure Log(LogType: JWSCLCom_TLB.JwEnumLogType; const LogMessage: WideString); safecall;
    procedure Signal(LogType: JWSCLCom_TLB.JwEnumSignalType; const LogMessage: WideString); safecall;
    procedure Memory(LogType: JWSCLCom_TLB.JwEnumMemoryType; const LogMessage: WideString); safecall;
    procedure Exception(Exception: OleVariant); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;


  TJwLogServerImpl = class(TAutoObject, JWSCLCom_TLB.IJwLogServer)
  protected
    fInternalObject : JwsclLogging.IJwLogServer;
  protected
    function Get_WriterClass: JWSCLCom_TLB.IJwWriterClass; safecall;
    procedure Set_WriterClass(const Value: JWSCLCom_TLB.IJwWriterClass); safecall;
    function Get_LogTypes: OleVariant; safecall;
    procedure Set_LogTypes(Value: OleVariant); safecall;
    function Connect(EnterType: JWSCLCom_TLB.JwEnumEnterType; const ClassName: WideString;
                     const MethodName: WideString; const FileName: WideString;
                     const LogMessage: WideString): JWSCLCom_TLB.IJwLogClient; safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;



implementation
{uses ComServ,  SysUtils, Dialogs,
     JwsclUtils, JwsclExceptions, Variants,
     JwsclCOMExports, JwsclTypes,
     JWSCLCoException;}

{ TJwLogServerImpl }

function TJwLogServerImpl.Connect(EnterType: JWSCLCom_TLB.JwEnumEnterType;
  const ClassName, MethodName, FileName,
  LogMessage: WideString): JWSCLCom_TLB.IJwLogClient;
begin

end;

destructor TJwLogServerImpl.Destroy;
begin

  inherited;
end;

function TJwLogServerImpl.Get_LogTypes: OleVariant;
begin

end;

function TJwLogServerImpl.Get_WriterClass: JWSCLCom_TLB.IJwWriterClass;
begin

end;

procedure TJwLogServerImpl.Initialize;
begin
  inherited;

  //fInternalObject := CreateLogServer()

end;

procedure TJwLogServerImpl.Set_LogTypes(Value: OleVariant);
begin

end;

procedure TJwLogServerImpl.Set_WriterClass(const Value: JWSCLCom_TLB.IJwWriterClass);
begin

end;

{ TJwLogClientImpl }

destructor TJwLogClientImpl.Destroy;
begin

  inherited;
end;

procedure TJwLogClientImpl.Exception(Exception: OleVariant);
begin

end;

procedure TJwLogClientImpl.Initialize;
begin
  inherited;

end;

procedure TJwLogClientImpl.Log(LogType: JWSCLCom_TLB.JwEnumLogType;
  const LogMessage: WideString);
begin

end;

procedure TJwLogClientImpl.Memory(LogType: JWSCLCom_TLB.JwEnumMemoryType;
  const LogMessage: WideString);
begin

end;

procedure TJwLogClientImpl.Signal(LogType: JWSCLCom_TLB.JwEnumSignalType;
  const LogMessage: WideString);
begin

end;

initialization
  try
  {TAutoObjectFactory.Create(ComServer, TJwLogServerImpl, CLASS_JwLogServer,
    ciMultiInstance, tmApartment);}
 { TAutoObjectFactory.Create(ComServer, TJwLogClientImpl, CLASS_JwLogClient,
    ciMultiInstance, tmApartment);}

{    TAutoObjectFactory.Create(ComServer, TTestInterfaceImpl, CLASS_TestInterface,
    ciMultiInstance, tmApartment);}
  except
    on E : Exception do
      ShowMessage(E.Message);
  end;


end.
