unit JwsclCOMExports;

interface
uses
  ComObj,
  ActiveX,
  Sysutils,
  Classes,
  Dialogs,
  JwsclSid,
  JwaWindows,
  JWSCLCoException,
  JwsclExceptions,
  JWSCLCom_TLB,
  JwsclResource,
  JwsclStrings;


procedure JwOleRaise(const Res : HRESULT); stdcall;
procedure JwOleRaiseEx(const E_In : Exception; out E_out : Exception);

function JwHasException(const Res : HRESULT) : BOOL; stdcall;

function CreateSidAndAttributesStream(const Sid : PSid; const Attributes : DWORD) : IStream; stdcall;

implementation
{$IFDEF SM_JCLDEBUG}
uses jclDebug;
{$ENDIF}         

function CreateSidAndAttributesStream(const Sid : PSid; const Attributes : DWORD) : IStream;
var S : TMemoryStream;
    SidSize : Integer;
begin
  if not IsValidSid(Sid) then
    RaiseLastOSError;

  S := TMemoryStream.Create;
  SidSize := GetLengthSID(Sid);

  S.Write(SidSize, sizeof(SidSize));

  S.Write(Sid^, SidSize);
  S.Write(Attributes, sizeof(Attributes));

  S.Position := 0;
  result := TStreamAdapter.Create(S,soOwned) as IStream;
  //TStreamAdapter frees the stream!!
end;


function JwHasException(const Res : HRESULT) : BOOL; stdcall;
begin
  result := Res = E_EXCEPTION;
end;


procedure JwOleRaiseEx(const E_In : Exception; out E_out : Exception);
var Err : IErrorInfo;
    Desc,
    StackTrace,
    Position : WideString;
    Exc : Exception;
    JExc : EJwsclSecurityException;
    List : TStringList;


{$IFDEF SM_JCLDEBUG}
    CallStackStrings: TStringList;
    pCaller : Pointer;
    sSourceFile,
    sModule,
    sSourceProc : String;
    iSourceLine : Integer;
    sI : TJclStackInfoList;
{$ENDIF SM_JCLDEBUG}
begin
  List := TStringList.Create;
  List.CommaText := E_In.Message;

  Exc := JwCreateException(List.Values[SJWEXCEPTIONNAME]).Create('');
  Exc.Message := List.Values[SJWMESSAGE];


{$IFDEF SM_JCLDEBUG}
  if DebugInfoAvailable(HInstance) then
  begin
    pCaller := Caller(0,false);
    if (MapOfAddr(pCaller,  sSourceFile, sModule, sSourceProc, Integer(iSourceLine))) then
    begin
      sSourceFile := JwFormatString(RsExceptionJCLText1,[sModule,sSourceFile]);

      CallStackStrings := TStringList.Create;

       sI := JclCreateStackList(false, 1, pCaller);
       if Assigned(sI) then
       begin
         SI.AddToStrings(CallStackStrings,
                        false,//IncludeModuleName: Boolean = False;
                        false,//IncludeAddressOffset: Boolean = False;
                        false,//IncludeStartProcLineOffset: Boolean = False;
                        false//IncludeVAdress: Boolean = False): Boolean;
                      ) ;

         if Exc is EJwsclSecurityException then
           (Exc as EJwsclSecurityException).StackTrace := CallStackStrings.Text;
       end;
       CallStackStrings.Free;
    end;
  end;
{$ENDIF}

  Position := 'COM Call:'+List.Values[SCOCLASSNAME]+'::'+List.Values[SCOMETHODNAME]+'('+
      List.Values[SCOSOURCEFILE]+':'+List.Values[SCOSOURCELINE]+')';
  StackTrace := '';

  if Exc is EJwsclSecurityException then
  begin
    JExc := Exc as EJwsclSecurityException;

    JExc.SourceProc := List.Values[SJWMETHODNAME];
    JExc.SourceClass := List.Values[SJWCLASSNAME];
    JExc.SourceFile := List.Values[SJWSOURCEFILE];
    JExc.SourceLine := StrToIntDef(List.Values[SJWSOURCELINE],0);
    JExc.LastError := StrToIntDef(List.Values[SJWGETLASTERROR],0);
    JExc.WinCallName := List.Values[SJWWINCALLNAME];

    if Length(JExc.StackTrace) > 0 then
       JExc.StackTrace := 'COM:'#13#10+List.Values[SJWSTACKTRACE] + #13#10'CLIENT:'#13#10+JExc.StackTrace
    else
      JExc.StackTrace := 'COM:'#13#10+List.Values[SJWSTACKTRACE];
    StackTrace := JExc.StackTrace;

    JExc.ComSource := Position;
  end;

  Exc.Message := Exc.Message +
    '********************************************************'#13#10+
    'JwOleRaiseEx added extra information for COM call:'#13#10#13#10+ Position;
  if Length(StackTrace) > 0 then
    Exc.Message := Exc.Message + #13#10#13#10'Stacktrace:'#13#10+JExc.StackTrace;

  E_out := Exc;
end;


procedure JwOleRaise(const Res : HRESULT);
var Err : IErrorInfo;
    Desc,
    Position : WideString;
    Exc : Exception;
    JExc : EJwsclSecurityException;
    List : TStringList;

begin
  if Succeeded(res) then
    exit;
    
  if Res = E_EXCEPTION then
  begin
    OleCheck(GetErrorInfo(0,Err));

    OleCheck(Err.GetDescription(Desc));


  end
  else
    OleError(Res);
end;

end.
