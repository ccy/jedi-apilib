{Jedi Windows Security Code Library Header File
Do not add DocOMatic comments into this file!}

//Freepascal compability mode (not fully supported)
{.$DEFINE FPC}

//use JEDI Component Library
{.$DEFINE JCL}

{Activate VISTA functions calls.
If this definition is set and the application is compiled with
static linked jwaVista.pas the binary application will fail
on pre Vista Windows versions!!

Warning: Unsetting this value will not remove constructor, method and property
definitions created for Windows Vista. It only removes function calls
so that static linked apps will not fail immediately on pre Vista systems.
Of course they will fail if Vista stuff is called on them.

Warning #2: If the JEDI API headers are used with a directive WIN_XP or lower
  this switch must be unset.
  These compiler directives reside in includes\jediapilib.inc
  Use at least WINVISTA or WIN2008
}
{$DEFINE VISTA}

//adds compiler version directives
//jwscl.inc is only added by JWSCL units in source folder
//so we're actually in the source folder and must jump up

//This is for compability reasons only!
//On of them should work. However the second one is standard and supported.
{.$I .\jedi.inc}
{$I ..\Includes\jedi.inc}


{$IFDEF FPC}
{$mode delphi}{.$H+}
{$ENDIF}

{$IFDEF DEBUG}
//
//Adds stack trace (using JCL) to JWSCL exceptions
{.$DEFINE JWSCL_DEBUG_INFO}
{$ENDIF DEBUG}

//Use cache mechanism for
// * TJwSecurityID.GetAccountSidString and property AccountName
{.$DEFINE JWSCL_USE_CACHES}

//Define this switch to allow showing a JWSCL task dialog when an exception is raised.
//In this way the default Delphi exception dialog is replaced.
//However, the VCL exception handling is not catched though. Instead, read the
//documentation of JwsclException.pas::JwHandleJwsclException and find out
//how to show the JWSCL dialog with VCL.
//The task dialog is only shown if the JEDI API source was compiled with compiler switch
//WIN2008 or WINVISTA and the running system is a Vista/2008 or newer.
{.$DEFINE JWSCL_USE_TASK_EXCEPTION_DIALOGS}

{$IFDEF JWSCL_USE_CACHES}
  {$DEFINE JWSCL_SIDCACHE}
{$ENDIF JWSCL_USE_CACHES}

//If JW_TYPEINFO is set, JWSCL uses the unit TypInfo
//See JWSCLUtils.pas GetUnitName
{$DEFINE JW_TYPEINFO}


{$IFDEF DELPHI2010_UP}
{$IFNDEF DEBUG}
  //Remove RTTI information for all JWSCL classes
  //D2010 adds it though
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF DEBUG}
{$ENDIF DELPHI2010_UP}

{$IFDEF DEBUG}
  {.$WARNINGS OFF}
{$IFDEF DELPHI7_UP}
  {$WARN GARBAGE OFF} //removes warning about code behind end.
{$ENDIF DELPHI7_UP}
{$ENDIF DEBUG}

//Remove range check errors since we are using
//variable length structures (like PSID) that fool Delphi
//See: http://stackoverflow.com/questions/1913460/delphi-rangecheckerror-in-jedi-wscl/1914245
{$R-}

//the following line must be at the end of this file!!
{Warning: This line is printed because an identifier has no pasdoc comment. This error must be fixed.}
