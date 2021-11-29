@echo off
if [%1]==[] goto usage
if [%2]==[] goto usage
if [%3]==[] goto usage

set folder=%1
set platform=%2
set config=%3

call ..\resources\copy-resources.cmd client %folder% %platform% %config%
rem call ..\resources\copy-library.cmd ssl %folder%
goto exit

:usage
echo.
echo Incorrect parameters
echo Configure Delphi Post Build Events
echo post-build.cmd "$(OutputDir)" $(Platform) $(Config)
goto error


:error
EXIT /B 1

:exit