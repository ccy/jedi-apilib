Read script files first in this order:

	setup.cmd
	dodiff.cmd

Before running SETUP.CMD assure Delpi bin directory is in PATH, example:

	PATH %PATH%;%ProgramFiles%\Borland\Delphi6\Bin

omitting PATH setup causes MAKE in SETUP.CMD to fail in which case SETUP.CMD
shall run with JEDIedit.exe file provided in JEDIedit.zip file.
