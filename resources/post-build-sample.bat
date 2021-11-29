@echo off
if [%1]==[] goto usage
if [%2]==[] goto usage
if [%3]==[] goto usage

set folder=%1
set platform=%2
set config=%3

call ..\..\..\resources\copy-resources.cmd client %folder% %platform% %config%
call ..\..\..\resources\copy-library.cmd mysql %folder%
call ..\..\..\resources\copy-library.cmd 7zip %folder%
call ..\..\..\resources\copy-library.cmd hunspell %folder%
call ..\..\..\resources\copy-library.cmd freeimage %folder%
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