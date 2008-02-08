unit JWSCLCoException;

interface
uses  ComObj, ActiveX, Sysutils,
      JWSCLExceptions,
      JWSCLStrings;


function JwSetCoException(const MethodName, ClassName, UnitName : WideString;
  const ExceptionType : Exception;
  out CoResult : HRESULT) : HRESULT;


const
    E_FAIL                           = HRESULT($80004005);
    E_EXCEPTION                      = HRESULT($80004005);

const LIBID_EXCEPTION : TGUID = '{305BF594-58DF-4E9B-B926-C3B447B9130D}';

implementation
uses IniFiles;



function JwFormatString(const Str : TJwString; const Args: array of const) : TJwString;
begin
  result := Sysutils.WideFormat(Str, Args);
  JwReplaceBreaks(result);
end;


function JwSetCoException(const MethodName, ClassName, UnitName : WideString;
  const ExceptionType : Exception;
  out CoResult : HRESULT) : HRESULT;
var
  ErrorInfo: ICreateErrorInfo;
  Description : WideString;
//  Ini : TIniFile;
begin
  CoResult := E_EXCEPTION;

  OleCheck(CreateErrorInfo(ErrorInfo));

  if ExceptionType is EJwsclSecurityException then
  begin
    Description := JwFormatString(
      '%s\%s\%s\123',
     [MethodName, ClassName, UnitName]);
  end
  else
  if ExceptionType is Exception then
  begin
    Description := JwFormatString(
      '%s\%s\%s\',
     [MethodName, ClassName, UnitName]);
  end;

  //OleCheck(ErrorInfo.SetGUID(LIBID_EXCEPTION));
  OleCheck(ErrorInfo.SetDescription(PWideChar(Description)));
  OleCheck(ErrorInfo.SetHelpContext(1));
  OleCheck(SetErrorInfo(0, (ErrorInfo as IErrorInfo)));
end;

end.
