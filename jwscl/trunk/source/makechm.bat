call makehelp.bat -O htmlhelp
del ".\doc\Security Manager Suite.chm"

REM call html help compiler
"C:\Programme\HTML Help Workshop\hhc.exe" ".\pasdoc\Security Manager Suite.hhp"

copy /Y ".\pasdoc\Security Manager Suite.chm" .\doc
pause
start hh.exe ".\pasdoc\Security Manager Suite.chm"
pause