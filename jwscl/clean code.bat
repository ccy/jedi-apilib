@echo off
echo This batch cleans up the code. It removes whitspaces and tabs which are not necessary 
echo   and may get between diffs.
echo This batch need the JEDIEdit project to be compiled from the JWA trunk repository!

echo on
..\jwapi\Trunk\Tools\JEDIedit\JEDIEdit.exe -i "clean code.ini"

pause