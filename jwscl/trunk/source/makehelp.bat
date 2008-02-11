REM to use this script you need PasDOC
REM Download it from http://pasdoc.sipsolutions.net/
REM and put Pasdoc.exe in the same folder as the source 
REM
REM call this script with "-O htmlhelp" to create HTML Help project files

mkdir pasdoc
REM pasdoc %1 %2 -N "JWSCL Help" -T "JEDI Windows Security Code Library" --write-uses-list --introduction "..\documentation\introduction.txt" --conclusion "..\documentation\conclusion.txt" --auto-link -D PACKAGE_CONDITIONS;JWA_INCLUDE_SETUP_API;JWA_INCLUDE_JWAADSTLB;DYNAMIC_LINK -I ..\..\..\jwapi\trunk\Common -E .\pasdoc *.pas ..\..\..\jwapi\trunk\Win32API\JwaWindows.pas ..\..\..\jwapi\trunk\Win32API\JwaVista.pas 
REM pasdoc %1 %2 -N "JWSCL Help" -T "JEDI Windows Security Code Library" --write-uses-list --introduction "..\documentation\introduction.txt" --conclusion "..\documentation\conclusion.txt" --auto-link -D PACKAGE_CONDITIONS;JWA_INCLUDE_SETUP_API;JWA_INCLUDE_JWAADSTLB;DYNAMIC_LINK -I ..\..\..\jwapi\trunk\Common;..\..\..\jwapi\trunk\Win32API\ -E .\pasdoc ..\..\..\jwapi\trunk\Win32API\JwaWindows.pas
pasdoc %1 %2 -N "JWSCL Help" -T "JEDI Windows Security Code Library" --write-uses-list --introduction "..\documentation\introduction.txt" --conclusion "..\documentation\conclusion.txt" --auto-link -D PACKAGE_CONDITIONS;JWA_INCLUDE_SETUP_API;JWA_INCLUDE_JWAADSTLB;DYNAMIC_LINK -I ..\..\..\jwapi\trunk\Common -E .\pasdoc *.pas "..\..\..\jwapi\trunk\Win32API\JwaVista.pas"

REM overwrite default style file with the one of JWSCL
copy /Y "JWSCLHelpBlue.css" .\pasdoc\pasdoc.css

pause

