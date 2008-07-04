{
Description
Project JEDI Windows Security Code Library (JWSCL)

<Description here>

Author
<Author name>

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

Note
The Original Code is Jwscl<UnitName>.pas.

The Initial Developer of the Original Code is <Author Name>


}
unit _JwsclTemplate;

{$INCLUDE Jwscl.inc}

interface
uses
  JwaWindows,      //JEDI API unit
  //more custom units here
  Classes,
  SysUtils,

  //mote  JWSCL units here
  JwsclUtils,      //utility functions like thread, localization, memory, exception utils
  JwsclExceptions, //exception classes
  JwsclResource,   //resource strings
  JwsclConstants,  //all JWSCL constant
  JwsclTypes,      //JWSCL types
  JwsclStrings;    //TJwString, TJwPChar, JwCompareString, JwFormatString, LoadLocalizedString

implementation

{
Please read the lines below to get an understanding how to write your code.

+At first read the file : "JEDI StyleGuide.pdf" from the svn folder jwscl/trunk/documentation
+Try to stick to it as best as possible

+Don't use a source formatter that uses a format that differs too much from the existing
source format

+JWSCL can be compiled with Delphi 7 and newer. Do not use new Delphi constructs
 without using compiler IFDEFS defined in jedi.inc (included by jwscl.inc)
 like DELPHI11_UP.
 If you want to use a new Delphi construct you must make sure that a programmer
 with an old Delphi version has also the possibility to use the feature in another
 way.
+There is no FreePascal support at the moment necessary.


+Add your exceptions to JwsclExceptions.
+Add your simple types to JwsclTypes.
+Add your constants to JwsclConstants.
+Add util functions to JwsclUtils.
+Add COM utils to JwsclComUtils.

+Name your classes with Jw : TJwYourClass
+Use TJwString and TJwPChar if you support Ansi- and Unicode. Otherwise use
AnsiString, WideString and PAnsiChar and PWideChar instead of simple String and PChar.
+Add string constants to JwsclResource.
Use the whole power of the exception constructors:

example for a failed windows api function call
  raise EJwsclWinCallFailedException.CreateFmtWinCall(
            RsSecurityDescriptorInvalid,          //Message as a resource string
            'GetJobObjectInformationLength',      //Source procedure name
            ClassName,                            //Source class name
            RsUNSid,                              //Source unit file name
            0,                                    //Source line (set to 0)
            True,                                 //add GetLastError information?
            'QueryInformationJobObject',          //Windows API function name that failed
            ['...'])                              //format string parameters for message

+Instead of returning an error code, always raise an exception!
+Create your own meaningful exception classes derived from EJwsclSecurityException or
 any of it descendants.
+Never use the EJwsclSecurityException in a raise statement.
+Do not use WinAPI Calls in a JwsclException call since it may change the GetLastError
value befor the constructor can read it. Instead save the GetLastError manually in a variable
and reset it by calling SetLastError.



Header
+adapt the unit header if necessary. Add your name as a (co)-author
+add description of the unit
+add a description of known bugs


Documentation
+document your code if it is problematic to understand
+document all constants, functions, procedures, types, classes,
  protected, public and published methods and properties. It is not necessary to document
  private parts that are only helper methods or variables for properties (which
  are already documented).
+Use XML and/or JavaDoc documentation style. The help creator Doc-O-Matic is used to
grab all documentation and to create a documentation file.

Example (pls use only comment brackets "{" for documentation):
More examples can be found at the Doc-O-Matic site http://www.doc-o-matic.com/examplesourcecode.html
Be aware that the JWSCL doc headers may differ from the shown examples because
they were translated from another (incompaticle) comment style.


<B>IsProcessInJob</B> returns whether a process is assigned to the job or not.
@param hProcess defines any handle to the process that is tested for membership.
@param Returns tre if the process is a member of the job; otherwise false.
@return Returns true if the given process is assigned to the current job instance; otherwise false.
raises
  EJwsclWinCallFailedException: can be raised if the call to an winapi function failed.

for the function:
function IsProcessInJob(hProcess : TJwProcessHandle) : Boolean;

+Add all exceptions that can be raised into the documentation even if they can be raised
from used methods. In the last case you can list all used methods instead so the user
can see which exception may be raised additionally.


Class Implementation
+ Don't use private declarations if there isn't a very good reason. First ask your tutor.
+ Stick to this class example

type
  TJwMyClass = class(...)  //see Jw ?
  protected    //we use protected, so derived classes can use it
    fVariable : Integer; //see f the and big letter?

    //you could add documentation here
    //but it is
    procedure SetVariable(const Value : Integer);
  public

    property Variable : Integer read fVariable write SetVariable;
  end;



}

end.
