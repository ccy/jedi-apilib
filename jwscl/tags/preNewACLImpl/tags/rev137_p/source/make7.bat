E:\Proggen\Borland\Delphi7\Bin\dcc32.exe -B -N.\dcu7 SecurityLibraryPackage_Delphi7.dpk >> Make.out.txt
if NOT ERRORLEVEL 0 goto exit

E:\Proggen\Borland\Delphi7\Bin\dcc32.exe -B -DSM_UNICODE -N.\dcu7\unicode SecurityLibraryPackage_Delphi7.dpk >> Make.out.txt
if NOT ERRORLEVEL 0 goto exit