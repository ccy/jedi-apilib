E:\Proggen\Borland\BDS\4.0\Bin\dcc32.exe -B -N.\dcu2006 SecurityLibraryPackage_BDS2006.dpk >> Make.out.txt
if NOT ERRORLEVEL 0 goto exit


E:\Proggen\Borland\BDS\4.0\Bin\dcc32.exe -B -DSM_UNICODE -N.\dcu2006\unicode SecurityLibraryPackage_BDS2006.dpk >> Make.out.txt
if NOT ERRORLEVEL 0 goto exit