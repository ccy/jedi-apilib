+new Exceptionhandling?:
In addition/contrast to current implementation* of COM Exception handling in JwsclException 
we can override SafeCallException for every COM class implementation. It converts a
raised exception by JWSCL for every defined safecall method.

SafeCallException
http://docs.codegear.com/docs/radstudio/radstudio2007/RS2007_helpupdates/HUpdate4/DE/html/delphivclwin32/System__TObject__SafeCallException.html

*The current implementation defines GUID for every exception in variable JwExceptionMapping.
The JWSCL exception is formatted for use in IErrorInfo (COM) :
 The IErrorInfo cannot contain all necessary information. Only the following information are directly converted:
   IErrorInfo member | Value
   -------------------------------------------------------------------------   
    GUID            | Exception classname (converted by func JwMapException)
	Source Pos      | A special formatted stack trace string (see source JWSCLCoException.pas::JwSetCoException)
	HelpContext     | always 1
	
  Other values are stored in Description member:	
	
	Description     | contains a comma separated TStringList
 
  The following values are supported in TStringList.Values[xxx] (defined in JWSCLCoException.pas)
    ++ : This value is only supported if the exception is a EJwsclSecurityException or a descendant of EJwsclSecurityException.
 
    SJWEXCEPTIONNAME = 'ExceptionName';
    SJWMETHODNAME = 'JwMethodName'; ++
    SJWCLASSNAME = 'JwClassName';
    SJWSOURCEFILE = 'JwSourceFile'; ++
    SJWSOURCELINE = 'JwSourceLine'; ++
    SJWGETLASTERROR = 'JwGetLastError'; ++
    SJWWINCALLNAME = 'JwWinCallName'; ++
    SJWMESSAGE = 'JwMessage';
	
    SCOMETHODNAME = 'CoMethodName';
    SCOCLASSNAME = 'CoClassName';
    SCOSOURCEFILE = 'CoSourceFile';
    SCOSOURCELINE = 'CoSourceLine';
    SJWSTACKTRACE = 'JwStackTrace'; ++
  

 
