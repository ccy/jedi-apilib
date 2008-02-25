REM to use this script you need the Microsoft HTML Help Compiler
REM ADAPT path to your help compiler!!
SET HHC_PATH="C:\Programme\HTML Help Workshop\hhc.exe"

call makehelp.bat -O htmlhelp
del "..\documentation\JWSCL help.chm"

REM call html help compiler
%HHC_PATH% ".\pasdoc\JWSCL help.hhp"

copy /Y ".\pasdoc\JWSCL help.chm" ..\documentation
pause
start hh.exe "..\documentation\JWSCL help.chm"
pause