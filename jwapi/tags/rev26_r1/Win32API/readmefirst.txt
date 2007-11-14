This is the try to include all converted header files into one unit.

PLEASE READ THIS FIRST!


To compile the bunch of files do these simple steps:

1. create a package with a name and location of your choice
2. add the file jwaWindows.pas - and only this file to the package
3. add the path to the common files (usually located in a folder named Common)
4. Choose between dynamic or static binding by set the compiler switch
 DYNAMIC_LINK or unset it.
You can change this in jwaWindows.pas. Open it to edit.
5. Choose an output path for the files that will be created by Delphi. You can change it in the setting/options dialog of the package.
7. Also set the options optimization and debugger option in the setting page as you want.
8. Compile the package. It will need its time.
9. If everything went fine you'll find a file called jwaWindows.dcu that you can use in your programs. Remember each delphi version needs its own version of that file. You have to repeat the compilation in each delphi version you want to use.
The advantage is that you can rebuilt your project without rebuilding all API files.

To use jwaWindows.dcu you can copy it in your project folder or
Add a search path into your project settings.

If you also add the source file folder to the debug path in your project settings, you can browse the source code without compiling it.


August 2007
Christian Wimmer (dezipaitor(at)gmx.de)
