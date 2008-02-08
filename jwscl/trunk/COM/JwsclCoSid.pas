unit JwsclCoSid;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, StdVcl, JWSCLCom_TLB,
  JwaWindows,
  JWSCLSid;
type
  TJwCoSid = class(TTypedComObject, IJwCoSid, ISupportErrorInfo)
  protected
    fInternalSid : TJwSecurityID;
  protected
    function InitBySid(SidPtr: PCoSid): HResult; stdcall;
    function GetSidPtr(out SidDataPtr: PCoSid; out SidDataSize: Integer): HResult; stdcall;
    function InitByStream(const SidAsStream: IUnknown): HResult; stdcall;
    function GetSidStream(out SidAsStream: IUnknown): HResult; stdcall;
    {IJwCoSid-Methoden hier deklarieren}

    function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
  end;

implementation


uses ComServ, TypInfo, SysUtils, Dialogs,
     JwsclUtils,
     JWSCLCoException;

function GetUnitName(argObject: TObject): string;
var
  ptrTypeData: PTypeData;
begin
  if (argObject.ClassInfo <> nil) then
  begin
    ptrTypeData := GetTypeData(argObject.ClassInfo);
    Result := ptrTypeData.UnitName;
  end;
end;


{ TJwCoSid }

function TJwCoSid.GetSidPtr(out SidDataPtr: PCoSid;
  out SidDataSize: Integer): HResult;
var Data : PSid;
begin
  result := S_OK;
 
  if not Assigned(fInternalSid) then
    result := MakeResult(1,FACILITY_JWSCL,  1);


  try
    SidDataPtr := CoTaskMemAlloc(SECURITY_MAX_SID_SIZE);
    SidDataSize := SECURITY_MAX_SID_SIZE;

    Data := fInternalSid.CreateCopyOfSID;
    CopyMemory(SidDataPtr, Data, SECURITY_MAX_SID_SIZE);
    fInternalSid.FreeSID(Data);
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSid.GetSidStream(out SidAsStream: IInterface): HResult;
begin

end;

function TJwCoSid.InitBySid(SidPtr: PCoSid): HResult;
begin
  result := S_OK;
  try
    result := S_FALSE;
    JwRaiseOnNilParameter(nil, '123','456','789',GetUnitName(Self));


    fInternalSid := TJwSecurityId.Create(PSid(SidPtr));
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSid.InitByStream(const SidAsStream: IInterface): HResult;
begin

end;

function TJwCoSid.InterfaceSupportsErrorInfo(const iid: TIID): HResult;
begin
  if GetInterfaceEntry(iid) <> nil then
    Result := S_OK else
    Result := S_FALSE;
end;

initialization
  TTypedComObjectFactory.Create(ComServer, TJwCoSid, Class_JwCoSid,
    ciMultiInstance, tmApartment);
end.
