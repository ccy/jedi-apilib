unit JWSCLCoException;

interface
uses  ComObj, ActiveX, Sysutils,
      JWSCLExceptions,Dialogs,
      JWSCLStrings;


function JwSetCoException(const MethodName, ClassName, UnitName : WideString;
  const ExceptionType : Exception;
  out CoResult : HRESULT) : HRESULT;

procedure JwTrapAndReRaiseException(const CoMethodName, CoClassName, CoUnitName : WideString;
  const ExceptionType : Exception);

const
    E_FAIL                           = HRESULT($80004005);
    E_EXCEPTION                      = HRESULT($80004005);

const LIBID_EXCEPTION : TGUID = '{305BF594-58DF-4E9B-B926-C3B447B9130D}';


const
    SJWEXCEPTIONNAME = 'ExceptionName';
    SJWMETHODNAME = 'JwMethodName';
    SJWCLASSNAME = 'JwClassName';
    SJWSOURCEFILE = 'JwSourceFile';
    SJWSOURCELINE = 'JwSourceLine';
    SJWGETLASTERROR = 'JwGetLastError';
    SJWWINCALLNAME = 'JwWinCallName';
    SJWMESSAGE = 'JwMessage';
    SCOMETHODNAME = 'CoMethodName';
    SCOCLASSNAME = 'CoClassName';
    SCOSOURCEFILE = 'CoSourceFile';
    SCOSOURCELINE = 'CoSourceLine';
    SJWSTACKTRACE = 'JwStackTrace';


implementation
uses IniFiles, Classes;



function JwFormatString(const Str : TJwString; const Args: array of const) : TJwString;
begin
  result := Sysutils.WideFormat(Str, Args);
  JwReplaceBreaks(result);
end;


procedure JwTrapAndReRaiseException(const CoMethodName, CoClassName, CoUnitName : WideString;
  const ExceptionType : Exception);
var
  JE : EJwsclSecurityException;
  ErrorInfo: ICreateErrorInfo;
  Position,
  Description : WideString;
  Data : TStringList;
begin
  Data := TStringList.Create;

  
  if ExceptionType is EJwsclSecurityException then
  begin
    JE := ExceptionType as EJwsclSecurityException;

    Data.Values[SJWEXCEPTIONNAME] := JE.ClassName;
    Data.Values[SJWMETHODNAME] := JE.SourceProc;
    Data.Values[SJWCLASSNAME] := JE.SourceClass;
    Data.Values[SJWSOURCEFILE] := JE.SourceFile;
    Data.Values[SJWSOURCELINE] := IntToStr(JE.SourceLine);
    Data.Values[SJWGETLASTERROR] := IntToStr(JE.LastError);
    Data.Values[SJWWINCALLNAME] := JE.WinCallName;
    Data.Values[SJWMESSAGE] := JE.Message;
    Data.Values[SCOMETHODNAME] := CoMethodName;
    Data.Values[SCOCLASSNAME] := CoClassName;
    Data.Values[SCOSOURCEFILE] := CoUnitName;
    Data.Values[SCOSOURCELINE] := '0';
    Data.Values[SJWSTACKTRACE] := JE.StackTrace;

    Position := JwFormatString('COM:%s::%s(%s:%d);JWSCL:%s::%s(%s:%d)',
        [CoClassName, CoMethodName, CoUnitName, 0,
         JE.SourceClass, JE.SourceProc, JE.SourceFile, JE.SourceLine
        ]);

  end
  else
//  if ExceptionType is Exception then
  begin
    Data.Values[SJWEXCEPTIONNAME] := ExceptionType.ClassName;
    Data.Values[SJWCLASSNAME] := IntToStr(GetLastError);
    Data.Values[SJWMESSAGE] := ExceptionType.Message;
    Data.Values[SCOMETHODNAME] := CoMethodName;
    Data.Values[SCOCLASSNAME] := CoClassName;
    Data.Values[SCOSOURCEFILE] := CoUnitName;
    Data.Values[SCOSOURCELINE] := '0';

    Position := JwFormatString('COM:%s::%s(%s:%d)',
        [CoClassName, CoMethodName, CoUnitName, 0  ]);
  end;

  Description := Data.CommaText;
  Data.Free;
  
  raise EOleSysError.Create(Description, E_FAIL, -1);
end;


function JwSetCoException(const MethodName, ClassName, UnitName : WideString;
  const ExceptionType : Exception;
  out CoResult : HRESULT) : HRESULT;
var
  JE : EJwsclSecurityException;
  ErrorInfo: ICreateErrorInfo;
  Position,
  Description : WideString;
  Data : TStringList;


begin
  CoResult := E_EXCEPTION;

  Data := TStringList.Create;

  OleCheck(CreateErrorInfo(ErrorInfo));


  if ExceptionType is EJwsclSecurityException then
  begin
    JE := ExceptionType as EJwsclSecurityException;

    Data.Values[SJWEXCEPTIONNAME] := JE.ClassName;
    Data.Values[SJWMETHODNAME] := JE.SourceProc;
    Data.Values[SJWCLASSNAME] := JE.SourceClass;
    Data.Values[SJWSOURCEFILE] := JE.SourceFile;
    Data.Values[SJWSOURCELINE] := IntToStr(JE.SourceLine);
    Data.Values[SJWGETLASTERROR] := IntToStr(JE.LastError);
    Data.Values[SJWWINCALLNAME] := JE.WinCallName;
    Data.Values[SJWMESSAGE] := JE.Message;
    Data.Values[SCOMETHODNAME] := MethodName;
    Data.Values[SCOCLASSNAME] := ClassName;
    Data.Values[SCOSOURCEFILE] := UnitName;
    Data.Values[SCOSOURCELINE] := '0';

    Position := JwFormatString('COM:%s::%s(%s:%d);JWSCL:%s::%s(%s:%d)',
        [ClassName, MethodName, UnitName, 0,
         JE.SourceClass, JE.SourceProc, JE.SourceFile, JE.SourceLine
        ]);

  end
  else
//  if ExceptionType is Exception then
  begin
    Data.Values[SJWEXCEPTIONNAME] := ExceptionType.ClassName;
    Data.Values[SJWCLASSNAME] := IntToStr(GetLastError);
    Data.Values[SJWMESSAGE] := ExceptionType.Message;
    Data.Values[SCOMETHODNAME] := MethodName;
    Data.Values[SCOCLASSNAME] := ClassName;
    Data.Values[SCOSOURCEFILE] := UnitName;
    Data.Values[SCOSOURCELINE] := '0';

    Position := JwFormatString('COM:%s::%s(%s:%d)',
        [ClassName, MethodName, UnitName, 0  ]);
  end;

  OleCheck(ErrorInfo.SetGUID(JwMapException(ExceptionType.ClassName)));
  OleCheck(ErrorInfo.SetSource(PWideChar(Position)));
   Description := Data.CommaText;
  OleCheck(ErrorInfo.SetDescription(PWideChar(Description)));
  OleCheck(ErrorInfo.SetHelpContext(1));
  OleCheck(SetErrorInfo(0, (ErrorInfo as IErrorInfo)));

  Data.Free;
end;

end.
