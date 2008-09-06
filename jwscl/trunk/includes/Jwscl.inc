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
}
{$DEFINE VISTA}

//adds compiler version directives
//jwscl.inc is only added by JWSCL units in source folder
//so we're actually in the source folder and must jump up
{$I ..\include\jedi.inc}

{$IFDEF FPC}
{$mode delphi}{.$H+}
{$ENDIF}

{$IFDEF DEBUG}
  {.$WARNINGS OFF}
{$ENDIF DEBUG}


//the following line must be at the end of this file!!
{Warning: This line is printed because an identifier has no pasdoc comment. This error must be fixed.}
