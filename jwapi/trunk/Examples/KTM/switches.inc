(**
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}

    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN SYMBOL_LIBRARY OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
    {$WARN UNIT_DEPRECATED OFF}
    {$WARN UNIT_LIBRARY OFF}
    {$WARN UNIT_PLATFORM OFF}
**)

{$DEFINE BP7}
{$DEFINE DELPHI1}
{$DEFINE DELPHI2}
{$DEFINE DELPHI3}
{$DEFINE DELPHI4}
{$DEFINE DELPHI5}
{$DEFINE DELPHI6}
{$DEFINE DELPHI7}
{$DEFINE DELPHI8}
{$DEFINE DELPHI2005}
{$DEFINE DELPHI2006}
{$DEFINE DELPHI2007}
{$DEFINE DELPHI2009}
{$DEFINE DELPHI2010}
{$DEFINE DELPHI_XE}

{* Borland Pascal 7 *}
{$IFDEF VER70}
    {$UNDEF DELPHI1}
    {$UNDEF DELPHI2}
    {$UNDEF DELPHI3}
    {$UNDEF DELPHI4}
    {$UNDEF DELPHI5}
    {$UNDEF DELPHI6}
    {$UNDEF DELPHI7}
    {$UNDEF DELPHI8}
    {$UNDEF DELPHI9}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 1.0 *}
{$IFDEF VER80}
    {$UNDEF DELPHI2}
    {$UNDEF DELPHI3}
    {$UNDEF DELPHI4}
    {$UNDEF DELPHI5}
    {$UNDEF DELPHI6}
    {$UNDEF DELPHI7}
    {$UNDEF DELPHI8}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 2.0 *}
{$IFDEF VER90}
    {$UNDEF DELPHI3}
    {$UNDEF DELPHI4}
    {$UNDEF DELPHI5}
    {$UNDEF DELPHI6}
    {$UNDEF DELPHI7}
    {$UNDEF DELPHI8}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 3.0 *}
{$IFDEF VER100}
    {$UNDEF DELPHI4}
    {$UNDEF DELPHI5}
    {$UNDEF DELPHI6}
    {$UNDEF DELPHI7}
    {$UNDEF DELPHI8}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 4.0 *}
{$IFDEF VER120}
    {$UNDEF DELPHI5}
    {$UNDEF DELPHI6}
    {$UNDEF DELPHI7}
    {$UNDEF DELPHI8}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 5.0 *}
{$IFDEF VER130}
    {$UNDEF DELPHI6}
    {$UNDEF DELPHI7}
    {$UNDEF DELPHI8}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 6.0 *}
{$IFDEF VER140}
    {$UNDEF DELPHI7}
    {$UNDEF DELPHI8}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 7.0 *}
{$IFDEF VER150}
// RTLVersion = 15.00;
    {$UNDEF DELPHI8}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 8.0 for .net *}
{$IFDEF VER160}
    {$UNDEF DELPHI2005}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{* Delphi 2005 / 9.0 for .net & Win32 *}
{$IFDEF VER170}
    {$UNDEF DELPHI2006}
    {$UNDEF DELPHI2007}
    {$UNDEF DELPHI2009}
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}
{$ENDIF}

{$IFDEF VER180}  // Delphi 2006 / Turbo Delphi / Delphi 2007
// RTLVersion = 18.00;
    {$IFDEF VER185}  // Delphi 2007 only
        {* Delphi 2007 for Win32 *}
        {$UNDEF DELPHI2009}
        {$UNDEF DELPHI2010}
        {$UNDEF DELPHI_XE}
    {$ELSE}
        {* Delphi 2006 / 10.0 for .net & Win32 *}
        {$UNDEF DELPHI2007}
        {$UNDEF DELPHI2009}
        {$UNDEF DELPHI2010}
        {$UNDEF DELPHI_XE}
    {$ENDIF}
{$ENDIF}

{* Delphi 2009 for Win32 *}
{$IFDEF VER200}
// RTLVersion = 20.00;
    {$UNDEF DELPHI2010}
    {$UNDEF DELPHI_XE}

    {$STRINGCHECKS OFF}
{$ENDIF}

{* Delphi 2010 for Win32 *}
{$IFDEF VER210}
// RTLVersion = 21.00;
    {$UNDEF DELPHI_XE}

    // http://docwiki.embarcadero.com/RADStudio/en/WEAKLINKRTTI_directive_%28Delphi%29
    {$WEAKLINKRTTI ON}
    // http://docwiki.embarcadero.com/RADStudio/en/RTTI_directive_%28Delphi%29
    // {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

    // {$HIGHCHARUNICODE ON}
    // http://docwiki.embarcadero.com/RADStudio/en/HIGHCHARUNICODE_directive_%28Delphi%29
    // When HIGHCHARUNICODE is ON: #$xx 2-digit and #$xxxx 4-digit literals are parsed as WideChar. 
{$ENDIF}

{* Delphi XE for Win32 *}
{$IFDEF VER220}
// RTLVersion = 22.00;
// COMPILERVERSION = 22.0
{$ENDIF}

{$IFDEF DELPHI7}
    {$IFDEF CLR}
        {$WARN UNSAFE_CODE ON}
        {$WARN UNSAFE_TYPE ON}
        {$WARN UNSAFE_CAST ON}
    {$ELSE}
        {$WARN UNSAFE_CODE OFF}
        {$WARN UNSAFE_TYPE OFF}
        {$WARN UNSAFE_CAST OFF}
    {$ENDIF}
{$ENDIF}

{$A+,B-,I-,P-,T+,V-,X+}

{$IFNDEF FPC}
    {$E+,F-,G+,K+,N-}
{$ENDIF}

{$IFDEF DEBUG}
    {$IFNDEF FPC}
        {$L+,Y+}
    {$ENDIF FPC}

    {$D+,C+,R+,S+,Q+,W+}
{$ELSE}
    {$IFDEF DEBUGINFO}
        {$IFNDEF FPC}
            {$L+,Y+}
        {$ENDIF FPC}

        {$D+,W+}
    {$ELSE}
        {$IFNDEF FPC}
            {$L-}
        {$ENDIF FPC}

        {$D-,W-}
    {$ENDIF}

    {$IFNDEF FPC}
        {$C-,Y-}
    {$ENDIF}

    {$R-,S-,Q-}
{$ENDIF}

{$IFDEF DELPHI2}
    {$J+}

    {$IFDEF DEBUG}
        {$OPTIMIZATION OFF}
    {$ELSE}
        {$IFDEF NO_OPT}
            {$OPTIMIZATION OFF}
        {$ELSE}
            {$OPTIMIZATION ON}
        {$ENDIF}
    {$ENDIF}

    {$LONGSTRINGS ON}
    {$HINTS ON}
    {$WARNINGS ON}
    {$J+}
{$ENDIF}

{$IFNDEF FPC}
    {$C Moveable Demandload Discardable} { Code Segment attributes }
{$ENDIF FPC}

