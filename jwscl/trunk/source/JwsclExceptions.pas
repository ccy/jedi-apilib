{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains exceptions that are used by the units of the JWSCL

Author
Christian Wimmer

License
The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

The Original Code is JwsclExceptions.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.


Remarks
Install JCL and compile this unit with compiler directive SM_JCLDEBUG and TD32 Debug Info.


}

{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclExceptions;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, Classes,
  jwaWindows,
  jwaVista,
  JwsclResource,
  JwsclTypes, JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  {<B>EJwsclSecurityException</B> is the main exception class that is used if an error occurs in any
        Security Library unit.
  }
  EJwsclSecurityException = class(Exception)
  protected
    fLastError:   Cardinal;
    fSourceProc, fsSourceClass,
    fsSourceFile: TJwString;
    fiSourceLine: Cardinal;
    fWinCallName: TJwString;
    fComSource : TJwString;
    fLog : TJwString;

    fStackTrace : TJwString;
  public
    constructor Create(const Msg: String); overload;
           {<B>CreateFmtEx</B> creates an instance of the @classname exception.
            @param sMsg contains a description of the exception. 
            @param sSourceProc contains the caller method name 
            @param sSourceClass contains the caller method class name 
            @param iSourceLine contains the caller source position 
            @param bShowLastError defines if windows GetLastError information is used 
            @param Args contains string formatting information for sMsg. 
            }
    constructor CreateFmtEx(const MessageString: TJwString;
      sSourceProc, sSourceClass,
      sSourceFile: TJwString; iSourceLine:
      Cardinal; bShowLastError: boolean;
      const Args: array of const);
      overload; virtual;
    constructor CreateFmtEx(const MessageString: TJwString;
      sSourceProc, sSourceClass,
      sSourceFile: TJwString; iSourceLine:
      Cardinal; iLastError: Cardinal;
      const Args: array of const);
      overload; virtual;
    constructor CreateFmtWinCall(const sMsg: TJwString;
      sSourceProc, sSourceClass,
      sSourceFile: TJwString; iSourceLine:
      Cardinal; bShowLastError: boolean;
      sWinCall: TJwString;
      const Args: array of const); virtual;
    constructor Create(const anException: EJwsclSecurityException);
      overload;

           {<B>GetErrorMessage</B> creates a windows error string from a LastError.
           @param errNumber contains a GetLastError error number. }
    class function GetErrorMessage(errNumber: TJwLastError): TJwString;
      virtual;

           {<B>GetLastErrorMessage</B> creates a windows error string from the last call to GetLastError or a defined error message.
            @param iGetLastError defines the error ID to be translated into a string. If set to high(Cardinal)
              the GetLastError() call is used.}
    class function GetLastErrorMessage(
      const iGetLastError: Cardinal = Cardinal(-1)): TJwString; virtual;

  public
    {<B>LastError</B> contains the LastError error code provided the the CreateFmtEx constructor}
    property LastError: Cardinal Read fLastError write fLastError;

    property SourceProc: TJwString Read fSourceProc Write fSourceProc;
    property SourceClass: TJwString
      Read fsSourceClass Write fsSourceClass;
    property SourceFile: TJwString
      Read fsSourceFile Write fsSourceFile;
    property SourceLine: Cardinal
      Read fiSourceLine Write fiSourceLine;

    {<B>WinCallName</B> defines the winapi function name of the failed call.}
    property WinCallName: TJwString Read fWinCallName Write fWinCallName;

    property Log : TJwString read fLog write fLog;
    property ComSource: TJwString read fComSource write fComSource;
    property StackTrace : TJwString read fStackTrace write fStackTrace;
  end;

  //<B>EJwsclOpenThreadTokenException</B> is raised if the thread token could not be opened
  EJwsclOpenThreadTokenException = class(EJwsclSecurityException);
  //<B>EJwsclOpenProcessTokenException</B> is raised if the process token could not be opened
  EJwsclOpenProcessTokenException = class(EJwsclSecurityException);
  //<B>EJwsclSharedTokenException</B> is raised is not used.
  EJwsclSharedTokenException = class(EJwsclSecurityException);
  //<B>EJwsclTokenInformationException</B> is raised if token information could not be retrieved.
  EJwsclTokenInformationException = class(EJwsclSecurityException);
  //<B>EJwsclTokenImpersonationException</B> is raised if the token could not be converted to an impersonated token.
  EJwsclTokenImpersonationException = class(EJwsclSecurityException);
  //<B>EJwsclTokenPrimaryException</B> is raised if the requested primary token could not be retrieved. For more information see LastError.
  EJwsclTokenPrimaryException = class(EJwsclSecurityException);
  EJwsclInvalidPrimaryToken = class(EJwsclTokenPrimaryException);


  //<B>EJwsclInvalidOwnerException</B> is raised if the given owner is invalid say nil.
  EJwsclInvalidOwnerException = class(EJwsclSecurityException);
  //<B>EJwsclDuplicateTokenException</B> is raised if a call to DuplicateTokenEx failed
  EJwsclDuplicateTokenException = class(EJwsclSecurityException);
  //<B>EJwsclNoThreadTokenAvailable</B> is raised if the requested impersonated token could not be retrieved
  EJwsclNoThreadTokenAvailable = class(EJwsclSecurityException);
  //<B>EJwsclInvalidTokenHandle</B> is raised if the handle of the token is invalid
  EJwsclInvalidTokenHandle = class(EJwsclSecurityException);
  //<B>EJwsclNotEnoughMemory</B> is raised if a allocation function could not allocate a buffer in memory because of not enough memory
  EJwsclNotEnoughMemory = class(EJwsclSecurityException);
  //<B>EJwsclPrivilegeException</B> is raised if an errors occurs that includes a problem with a privilege
  EJwsclPrivilegeException = class(EJwsclSecurityException);
  //<B>EJwsclInvalidIndexPrivilegeException</B> is raised if the given index is out of bounds of the privileges list
  EJwsclInvalidIndexPrivilegeException =
    class(EJwsclPrivilegeException);
  //<B>EJwsclPrivilegeNotFoundException</B> is raised if a given privilege was not found
  EJwsclPrivilegeNotFoundException = class(EJwsclPrivilegeException);
  //<B>EJwsclPrivilegeCheckException</B> is raised if a given privilege was not found in the list of privileges of the token
  EJwsclPrivilegeCheckException = class(EJwsclPrivilegeException);
  //<B>EJwsclAdjustPrivilegeException</B> is raised if the privileges of a token could not be changed
  EJwsclAdjustPrivilegeException = class(EJwsclPrivilegeException);
  //<B>EJwsclAccessTypeException</B> is raised if the desired access mask is not included in the token access mask!
  EJwsclAccessTypeException = class(EJwsclSecurityException);
  //<B>EJwsclNotImplementedException</B> is raised if the called method is not implemented yet.
  EJwsclNotImplementedException = class(EJwsclSecurityException);
  //<B>EJwsclUnsupportedWindowsVersionException</B> is raised if the called function is not supported under the running windows version
  EJwsclUnsupportedWindowsVersionException =  class(EJwsclSecurityException);




  //<B>EJwsclWinCallFailedException</B> is raised if a call to a windows API function failed. For more information see the LastError property
  EJwsclWinCallFailedException = class(EJwsclSecurityException);
  EJwsclProcessIdNotAvailable = class(EJwsclSecurityException);

  EJwsclInvalidObjectArrayException = class(EJwsclSecurityException);

  EJwsclInheritanceSourceNotSupportedException =
    class(EJwsclSecurityException);

  //<B>EJwsclNILParameterException</B> is raised if a given parameter is nil which is invalid.
  EJwsclNILParameterException = class(EJwsclSecurityException);
  EJwsclEmptyACLException = class(EJwsclSecurityException);
  EJwsclInvalidMandatoryLevelException = class(EJwsclSecurityException);

  EJwsclInvalidSecurityListException = class(EJwsclSecurityException);

  //<B>EJwsclInvalidSIDException</B> is raised if a SID has an invalid structure
  EJwsclInvalidSIDException = class(EJwsclSecurityException);
  EJwsclInvalidOwnerSIDException = class(EJwsclInvalidSIDException);
  EJwsclInvalidGroupSIDException = class(EJwsclInvalidSIDException);

  EJwsclInvalidComputer = class(EJwsclSecurityException);

  {<B>EJwsclInvalidKnownSIDException</B> is raised if TJwSecurityId.CreateWellKnownSid fails}
  EJwsclInvalidKnownSIDException = class(EJwsclInvalidSIDException)
  protected
    fSidType : TWellKnownSidType;
  public
    {<B>SidType</B> contains information which sid type was used
     but could not be found}
    property SidType : TWellKnownSidType read fSidType write fSidType;
  end;

  EJwsclInvalidSidAuthorityValue = class(EJwsclInvalidSIDException);


  //<B>EJwsclIndexOutOfBoundsException</B> is raised if an given index is not within the bounds of a list
  EJwsclIndexOutOfBoundsException = class(EJwsclSecurityException);

  //<B>EJwsclDuplicateListEntryException</B> is raised if a SID was already added to a list
  EJwsclDuplicateListEntryException = class(EJwsclSecurityException);

  EJwsclReadOnlyPropertyException = class(EJwsclSecurityException);

  EJwsclInvalidACEException = class(EJwsclSecurityException);
  EJwsclRevisionMismatchException = class(EJwsclSecurityException);
  EJwsclInvalidAceMismatch = class(EJwsclSecurityException);
  EJwsclInvalidRevision = class(EJwsclSecurityException);

  EJwsclInvalidSecurityDescriptor = class(EJwsclSecurityException);
  EJwsclInvalidPathException = class(EJwsclSecurityException);

  EJwsclInvalidParameterException = class(EJwsclSecurityException);

  EJwsclProcessNotFound = class(EJwsclSecurityException);

  EJwsclInvalidFlagsException = class(EJwsclSecurityException);
  EJwsclNoSuchLogonSession = class(EJwsclSecurityException);

  EJwsclStreamException = class(EJwsclSecurityException);
  EJwsclStreamSizeException = class(EJwsclStreamException);
  EJwsclStreamInvalidMagicException = class(EJwsclStreamException);

  EJwsclStreamHashException = class(EJwsclStreamException);

  //EHashMismatch is raised in case of unequal hash data
  EJwsclHashMismatch = class(EJwsclSecurityException);


  EJwsclSecurityObjectException = class(EJwsclSecurityException);
  EJwsclInvalidObjectException = class(EJwsclSecurityException);

  EJwsclThreadException = class(EJwsclSecurityException);
  EJwsclAdaptSecurityInfoException = class(EJwsclSecurityException);

  EJwsclInvalidGenericAccessMask = class(EJwsclSecurityException);

  EJwsclInvalidKeyPath = class(EJwsclSecurityException);
  EJwsclInvalidParentDescriptor = class(EJwsclSecurityException);

  ESetSecurityException = class(EJwsclSecurityException);
  ESetOwnerException = class(EJwsclStreamException);

  //<B>EJwsclExceptionClass</B> is the class of EJwsclSecurityException
  EJwsclExceptionClass = class of EJwsclSecurityException;

  EJwsclLSAException = class(EJwsclSecurityException);

  EJwsclAccessDenied = class(EJwsclSecurityException);
  EJwsclSACLAccessDenied = class(EJwsclAccessDenied);
        {
        EDesktopException is the general exception that is raised if an error occurred
        during desktop manipulation.
        }
  EJwsclDesktopException = class(EJwsclSecurityException);

        {EOpenDesktopException is raised if there was an error during opening a desktops.
        Possible cases are :
        1. Desktop does not exists
        }
  EJwsclOpenDesktopException = class(EJwsclDesktopException);

        {ECreateDesktopException is raised if there was an error during creating a new desktop.
        Possible cases are :
        1. Desktop already exists
        2. Not enough rights}
  EJwsclCreateDesktopException = class(EJwsclDesktopException);
        {ECloseDesktopException is raised if there was an error during closing a desktop.
        Possible cases are :
        1. Desktop handle is not valid
        2. not enough rights
        }

  EJwsclCloseDesktopException = class(EJwsclDesktopException);


  EJwsclWindowStationException = class(EJwsclSecurityException);

  EJwsclOpenWindowStationException = class(EJwsclWindowStationException);

  EJwsclUnsupportedACE = class(EJwsclSecurityException);
  EJwsclFailedAddACE = class(EJwsclSecurityException);

  EJwsclSecurityExceptionClass = class of EJwsclSecurityException;

  EJwsclResourceException = class(EJwsclSecurityException);
  EJwsclResourceNotFound = class(EJwsclResourceException);
  EJwsclResourceUnequalCount = class(EJwsclResourceException);
  EJwsclResourceInitFailed = class(EJwsclResourceException);

  EJwsclOSError = class(EJwsclSecurityException);

  EJwsclCryptException = class(EJwsclSecurityException);
  EJwsclCryptApiException = class(EjwsclCryptException);
  EJwsclCryptUnsupportedException = class(EjwsclCryptException);

  //general exception for terminal server methods
  EJwsclTerminalServerException = class(EJwsclSecurityException);
  // Terminal Server Connection Exception
  EJwsclTerminalServerConnectException = class(EJwsclTerminalServerException);

  EJwsclTerminalServiceException =  class(EJwsclSecurityException);
  EJwsclTerminalServiceNecessary =  class(EJwsclTerminalServiceException);

  //general exception for terminal session methods

  //general exception for terminal session methods
  EJwsclTerminalSessionException = class(EJwsclTerminalServerException);

  EJwsclCSPException = class(EJwsclCryptException);
  EJwsclCSPApiException = class(EJwsclCSPException);

  EJwsclHashException = class(EJwsclCryptException);
  EJwsclHashApiException = class (EJwsclHashException);

  EJwsclKeyException = class(EJwsclCryptException);
  EJwsclKeyApiException = class(EJwsclKeyException);

  {<B>EJwsclInitWellKnownException</B> is raised if JwInitWellKnownSIDs was not called.}
  EJwsclInitWellKnownException = class(EJwsclKeyException);

  EJwsclUnimplemented = class(EJwsclSecurityException);

  EJwsclNilPointer = class(EJwsclSecurityException);
  EJwsclCreateProcessFailed = class(EJwsclSecurityException);
  EJwsclInvalidPointerType = class(EJwsclSecurityException);

  EJwsclMissingEvent = class(EJwsclSecurityException);

  EJwsclInvalidSession = class(EJwsclSecurityException);
  EJwsclInvalidIndex = class(EJwsclSecurityException);

  EJwsclInvalidHandle = class(EJwsclSecurityException);
  EJwsclClassTypeMismatch = class(EJwsclSecurityException);

  EJwsclEndOfStream = class(EJwsclSecurityException);


  {}
  EJwsclEnumerateProcessFailed = class(EJwsclSecurityException);

  EJwsclGenericFirewallException = class(EJwsclSecurityException);

	  EJwsclFirewallInitException = class(EJwsclGenericFirewallException);
	  EJwsclFirewallProfileInitException = class(EJwsclGenericFirewallException);  

	  EJwsclSetFWStateException = class(EJwsclGenericFirewallException);
	  EJwsclGetFWStateException = class(EJwsclGenericFirewallException);

	  EJwsclGetFWExceptionsAllowedException = class(EJwsclGenericFirewallException);
	  EJwsclSetFWExceptionsAllowedException = class(EJwsclGenericFirewallException);

	  EJwsclGetIncommingPingAllowedException = class(EJwsclGenericFirewallException);
	  EJwsclSetIncommingPingAllowedException = class(EJwsclGenericFirewallException);

	  EJwsclGetRemoteAdminAllowedException = class(EJwsclGenericFirewallException);
	  EJwsclSetRemoteAdminAllowedException = class(EJwsclGenericFirewallException);

	  EJwsclGetRemoteAdminAdressException = class(EJwsclGenericFirewallException);
	  EJwsclSetRemoteAdminAdressException = class(EJwsclGenericFirewallException);

	  EJwsclFirewallAddRuleException = class(EJwsclGenericFirewallException);
	  EJwsclAddTcpPortToFirewallException = class(EJwsclGenericFirewallException);
	  EJwsclAddUdpPortToFirewallException = class(EJwsclGenericFirewallException);
	  EJwsclFirewallDelRuleException = class(EJwsclGenericFirewallException);

	  EJwsclFirewallInactiveException = class(EJwsclGenericFirewallException);
	  EJwsclFirewallNoExceptionsException = class(EJwsclGenericFirewallException); 



  EJwsclInvalidStartupInfo = class(EJwsclSecurityException);
  

  JwGeneralExceptionClass = class of Exception;


type
   TJwExceptionMapping = record
     Name : WideString;
     ID : TGUID;
     ExcPtr : JwGeneralExceptionClass;
   end;


const JwExceptionMapping : array[0..3] of TJwExceptionMapping =
      ((Name: 'Exception';
        ID: '{138EDC0B-B10B-4FA3-BB5D-DFDABBEBDDA4}';
        ExcPtr : Exception),
       (Name: 'EJwsclWinCallFailedException';
        ID: '{138EDC0B-B10B-4FA3-BB5D-DFDABBEBDDA5}';
        ExcPtr : EJwsclWinCallFailedException),
       (Name: 'EJwsclSecurityException';
        ID: '{138EDC0B-B10B-4FA3-BB5D-DFDABBEBDDA6}';
        ExcPtr : EJwsclSecurityException),
       (Name: 'EJwsclNILParameterException';
        ID: '{138EDC0B-B10B-4FA3-BB5D-DFDABBEBDDA7}';
        ExcPtr : EJwsclNILParameterException)
      );

function JwCreateException(const Name : WideString) : JwGeneralExceptionClass;
function JwMapException(Const Id : TGuid) : WideString; overload;
function JwMapException(Const Name : WideString) : TGuid; overload;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

{$IFDEF SM_JCLDEBUG}
uses jclDebug;
{$ENDIF}
{$ENDIF SL_OMIT_SECTIONS}




function JwMapException(Const Id : TGuid) : WideString;

  function CompareGUID(const G1, G2: TGUID): boolean;
  begin
    Result := CompareMem(@G1, @G2, Sizeof(TGUID));
  end;

var i : Integer;
begin
  result := '';
  for i := low(JwExceptionMapping) to high(JwExceptionMapping) do
  begin
    if CompareGUID(JwExceptionMapping[i].ID, Id) then
    begin
      result := JwExceptionMapping[i].Name;
      exit;
    end;
  end;
end;

function JwMapException(Const Name : WideString) : TGuid;
var i : Integer;
begin
  result := NULL_GUID;
  for i := low(JwExceptionMapping) to high(JwExceptionMapping) do
  begin
    if WideCompareText(Name, JwExceptionMapping[i].Name) = 0 then
    begin
      result := JwExceptionMapping[i].Id;
      exit;
    end;
  end;
end;

function JwCreateException(const Name : WideString) : JwGeneralExceptionClass;
var i : Integer;
begin
  result := Exception;
  for i := low(JwExceptionMapping) to high(JwExceptionMapping) do
  begin
    if WideCompareText(Name, JwExceptionMapping[i].Name) = 0 then
    begin
      result := JwExceptionMapping[i].ExcPtr;
      exit;
    end;
  end;
end;


{$IFNDEF SL_INTERFACE_SECTION}
constructor EJwsclSecurityException.Create(const Msg: String);
begin
  inherited Create(Msg);

  fLastError := GetLastError;
end;

constructor EJwsclSecurityException.CreateFmtWinCall(const sMsg: TJwString;
  sSourceProc, sSourceClass,
  sSourceFile: TJwString; iSourceLine:
  Cardinal; bShowLastError: boolean;
  sWinCall: TJwString;
  const Args: array of const);
begin
  CreateFmtEx(sMsg, sSourceProc, sSourceClass, sSourceFile,
    iSourceLine, bShowLastError, Args);
  fWinCallName := sWinCall;
end;

constructor EJwsclSecurityException.Create(
  const anException: EJwsclSecurityException);
begin
  inherited Create(anException.Message);

  fLastError := anException.fLastError;
  fSourceProc := anException.fSourceProc;
  fsSourceClass := anException.fsSourceClass;
  fsSourceFile := anException.fsSourceFile;
  fiSourceLine := anException.fiSourceLine;
  fWinCallName := anException.fWinCallName;
end;

constructor EJwsclSecurityException.CreateFmtEx(const MessageString: TJwString;
  sSourceProc, sSourceClass,
  sSourceFile: TJwString;
  iSourceLine: Cardinal;
  iLastError: Cardinal;
  const Args: array of const);
var
  aMessage, sData, sLastError,sJCLText : TJwString;
{$IFDEF SM_JCLDEBUG}
    CallStackStrings: TStringList;
    pCaller : Pointer;
    sModule: TJwString;
    sI : TJclStackInfoList;
{$ENDIF SM_JCLDEBUG}

begin
  fLastError := iLastError;
  fSourceProc := sSourceProc;
  fsSourceClass := sSourceClass;
  fsSourceFile := sSourceFile;
  fiSourceLine := fiSourceLine;

  if Length(MessageString) > 0 then
  begin
    sData := JwFormatString(RsExceptionMessage, [MessageString]);
    sData := JwFormatString(sData,Args);
  end
  else
    sData := '';

  if Length(sSourceProc) = 0 then
    sSourceProc := RsExceptionNoProc;

  if Length(sSourceClass) = 0 then
    sSourceClass := RsExceptionNoClass;

  if Length(sSourceFile) = 0 then
    sSourceFile := RsExceptionNoFile;


  sLastError := JwFormatString(RsExceptionErrors,
    [iLastError, IntToHex(iLastError, 1),
    GetLastErrorMessage(iLastError)]);

  sJCLText := '';
{$IFDEF SM_JCLDEBUG}

  if DebugInfoAvailable(HInstance) then
  begin
    pCaller := Caller(1,false);
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
         sJCLText := TJwString(CallStackStrings.Text);

         fStackTrace := TJwString(CallStackStrings.Text);
       end;
       CallStackStrings.Free;
    end;

  end;
{$ENDIF}

  aMessage := JwFormatString(RsExceptionMainMessage,
   [ClassName, sSourceProc, sSourceClass,
   sSourceFile, iSourceLine, sLastError, sData,sJCLText]);


  inherited Create(aMessage);
end;




constructor EJwsclSecurityException.CreateFmtEx(const MessageString: TJwString;
  sSourceProc, sSourceClass,
  sSourceFile: TJwString;
  iSourceLine:  Cardinal;
  bShowLastError: boolean;
  const Args: array of const);
var
  aMessage, sData, sLastError,
  sJCLText: TJwString;
{$IFDEF SM_JCLDEBUG}
    CallStackStrings: TStringList;
    pCaller : Pointer;
    sModule: TJwString;
    sI : TJclStackInfoList;
{$ENDIF SM_JCLDEBUG}
begin
  fLastError := GetLastError;
  fSourceProc := sSourceProc;
  fsSourceClass := sSourceClass;
  fsSourceFile := sSourceFile;
  fiSourceLine := fiSourceLine;



  if Length(MessageString) > 0 then
  begin
    sData := JwFormatString(RsExceptionMessage, [MessageString]);
    sData := JwFormatString(sData,Args);
  end
  else
    sData := '';

  if Length(sSourceProc) = 0 then
    sSourceProc := RsExceptionNoProc;

  if Length(sSourceClass) = 0 then
    sSourceClass := RsExceptionNoClass;

  if Length(sSourceFile) = 0 then
    sSourceFile := RsExceptionNoFile;


  if bShowLastError then
  begin
    sLastError := JwFormatString(RsExceptionErrors,
    [fLastError, IntToHex(fLastError, 1),
    GetLastErrorMessage(fLastError)]);
  end;

  aMessage := RsExceptionMainMessage;

  sJCLText := '';
{$IFDEF SM_JCLDEBUG}

  if DebugInfoAvailable(HInstance) then
  begin
    pCaller := Caller(1,false);
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
         sJCLText := TJwString(CallStackStrings.Text);
         fStackTrace := TJwString(CallStackStrings.Text);
       end;
       CallStackStrings.Free;
    end;
  end;
{$ENDIF}


  aMessage := JwFormatString(RsExceptionMainMessage,
   [ClassName, sSourceProc, sSourceClass,
   sSourceFile, iSourceLine, sLastError, sData, sJCLText]);


  inherited Create(aMessage);


end;


class function EJwsclSecurityException.GetLastErrorMessage(
  const iGetLastError: Cardinal = Cardinal(-1)): TJwString;
begin
  if iGetLastError = Cardinal(-1) then
    Result := GetErrorMessage(GetLastError)
  else
    Result := GetErrorMessage(iGetLastError);
end;

class function EJwsclSecurityException.GetErrorMessage(errNumber: TJwLastError)
: TJwString;
var
  s: TJwPChar;
//  i : DWORD;
begin
  if (
{$IFDEF UNICODE}
    FormatMessageW
{$ELSE}
    FormatMessageA
{$ENDIF}
    (FORMAT_MESSAGE_ALLOCATE_BUFFER or
    FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS //see http://blogs.msdn.com/oldnewthing/archive/2007/11/28/6564257.aspx
    ,
    nil,
    errNumber, 0, TJwPChar(@s),
    0, nil) = 0) then
  begin
    Result := RsUnknownGetLastError;
//    i := GetLasterror;
    exit;
  end;
  Result := s;

  LocalFree(HLOCAL(s));
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}


end.
{$ENDIF SL_OMIT_SECTIONS}