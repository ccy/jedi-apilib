{Jedi Windows Security Code Library Header File
Do not add pasdoc comments into this file!}

//Freepascal compability mode
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

{$INCLUDE Compilers.inc}

{$IFDEF FPC}
{$mode delphi}{.$H+}
{$ENDIF}

//the following line must be at the end of this file!!
{Warning: This line is printed because an identifier has no pasdoc comment. This error must be fixed.}
