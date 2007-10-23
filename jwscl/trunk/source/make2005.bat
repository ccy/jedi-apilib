E:\Proggen\Borland\BDS\3.0\Bin\dcc32.exe -B -N.\dcu2005 SecurityLibraryPackage_BDS2005.dpk >> Make.out.txt
if NOT ERRORLEVEL 0 goto exit

E:\Proggen\Borland\BDS\3.0\Bin\dcc32.exe -B -DSM_UNICODE -N.\dcu2005\unicode SecurityLibraryPackage_BDS2005.dpk >> Make.out.txt
if NOT ERRORLEVEL 0 goto exit
