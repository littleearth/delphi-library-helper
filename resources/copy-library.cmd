@echo off
if [%1]==[] goto usage
if [%2]==[] goto usage

set library=%1
set folder=%2
set source=%~dp0

xcopy "%source%libraries\%library%\%platform%\*" %folder% /E /C /Y
xcopy "%source%libraries\%library%\all\*" %folder% /E /C /Y

goto exit

:usage
echo.
echo Incorrect parameters
echo copy-library.cmd libname destination
echo eg mysql "C:\My Folder"
goto error


:error
EXIT /B 1

:exit