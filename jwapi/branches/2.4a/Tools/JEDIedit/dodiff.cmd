@echo off
pushd jwapi
svn diff       > ..\diff-jwapi-max.txt
svn diff -x -w > ..\diff-jwapi-min.txt
popd
pushd jwscl
svn diff       > ..\diff-jwscl-max.txt
svn diff -x -w > ..\diff-jwscl-min.txt
popd
