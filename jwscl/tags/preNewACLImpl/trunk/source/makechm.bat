REM to use this script you need the Microsoft HTML Help Compiler
REM 

call makehelp.bat -O htmlhelp
del "..\documentation\JWSCL help.chm"

REM call html help compiler
"C:\Programme\HTML Help Workshop\hhc.exe" ".\pasdoc\JWSCL help.hhp"

copy /Y ".\pasdoc\JWSCL help.chm" ..\documentation
pause
start hh.exe "..\documentation\JWSCL help.chm"
pause