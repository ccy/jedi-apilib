program Test1;

{$APPTYPE CONSOLE}

uses
  ComObj, ActiveX, Sysutils,
  JwsclSid, JwaWindows,
  JWSCLCom_TLB in '..\JWSCLCom_TLB.pas';
  

function JwCoConvertSid(const SidPtr : TJwSecurityId) : PCoSid;
var Data : PSID;
begin
  Data := SidPtr.CreateCopyOfSID;
  result := CoTaskMemAlloc(SECURITY_MAX_SID_SIZE);
  CopyMemory(result, Data, SECURITY_MAX_SID_SIZE);

  SidPtr.FreeSID(Data);
end;

procedure JwOleRaise(const Res : HRESULT);
var Err : IErrorInfo;
    Desc : WideString;
begin
  if Failed(Res) then
  begin
    OleCheck(GetErrorInfo(0,Err));

    OleCheck(Err.GetDescription(Desc));

    if Desc = '' then;

  end;
end;

var CoSid : IJwCoSid;
    Sid : TJwSecurityId;
    CoSidData : PCoSid;
begin
  CoInitialize(0);

  try 
    Sid := TJwSecurityId.Create('S-1-1-0');
    CoSidData := JwCoConvertSid(Sid);

    CoSid := CoJwCoSid.Create;
    JwOleRaise(CoSid.InitBySid(PCoSid(CoSidData)));

    CoTaskMemFree(CoSidData);
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;     
end.
