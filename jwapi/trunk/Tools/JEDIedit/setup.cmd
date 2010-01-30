@echo off

Rem Compile JEDIedit program

if not exist dcu md dcu
make -B JEDIedit.exe

Rem Checkout JEDI-APILIB files

svn co https://jedi-apilib.svn.sourceforge.net/svnroot/jedi-apilib/jwapi/trunk jwapi
svn co https://jedi-apilib.svn.sourceforge.net/svnroot/jedi-apilib/jwscl/trunk jwscl


Rem Run JEDIedit program

JEDIedit > Changed-Files-List.txt
