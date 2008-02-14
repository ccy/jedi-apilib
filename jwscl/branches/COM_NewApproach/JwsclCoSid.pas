unit JwsclCoSid;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, StdVcl, Classes, JWSCLCom_TLB,
  JwaWindows,TypInfo,
  JWSCLSid;
type
  TJwSid = class(TAutoObject, IJwSid)
  //TJwCoSid = class(TTypedComObject, IJwCoSid, ISupportErrorInfo)
  protected
    fInternalSid : TJwSecurityID;
    procedure InitByBinarySid(const BinarySid: WideString); safecall;
  protected
  public
   {IJwCoSid-Methoden hier deklarieren}
   procedure Initialize; override;
   destructor Destroy; override;

  end;


  TJwSidList = class(TAutoObject, IJwSidList)
  //TJwCoSidList = class(TTypedComObject, IJwCoSidList, ISupportErrorInfo)
  protected
    fInternalSidIntList : TInterfaceList;//TJwSecurityIdList;
    fInternalSidList : TJwSecurityIdList;
    procedure Method1; safecall;
  protected
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;


implementation


uses ComServ,  SysUtils, Dialogs,
     JwsclUtils,
     JWSCLCoException;

{ TJwCoSid }

destructor TJwSid.Destroy;
begin
  FreeAndNil(fInternalSid);
  inherited;
end;

procedure TJwSid.InitByBinarySid(const BinarySid: WideString);
begin
  FreeAndNil(fInternalSid);
  try
    fInternalSid := TJwSecurityID.Create(BinarySid);
  except
    on E : Exception do
      JwTrapAndReRaiseException('InitByBinarySid', ClassName, 'JwsclCoSid', E);
  end;
end;

procedure TJwSid.Initialize;
begin
  inherited;

  fInternalSid := TJwSecurityID.Create;
end;

{ TJwCoSidList }

destructor TJwSidList.Destroy;
begin

  inherited;
end;

procedure TJwSidList.Initialize;
begin
  inherited;

end;

procedure TJwSidList.Method1;
begin

end;

initialization


  TAutoObjectFactory.Create(ComServer, TJwSid, Class_JwSid,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwSidList, CLASS_JwSidList,
    ciMultiInstance, tmApartment);
end.
