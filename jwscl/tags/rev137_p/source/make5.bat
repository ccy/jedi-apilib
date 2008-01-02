
E:\Proggen\Borland\Delphi5\Bin\dcc32.exe -B -N.\dcu5 SecurityLibraryPackage_Delphi5.dpk >> Make.out.txt
if NOT ERRORLEVEL 0 goto exit

E:\Proggen\Borland\Delphi5\Bin\dcc32.exe -B -DSM_UNICODE -N.\dcu5\unicode SecurityLibraryPackage_Delphi5.dpk >> Make.out.txt
if NOT ERRORLEVEL 0 goto exit

